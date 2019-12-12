/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef unsigned int GLenum;
typedef unsigned char GLboolean;
typedef int GLint;
typedef unsigned short GLushort;
typedef unsigned int GLuint;
typedef float GLfloat;
typedef GLushort GLchan;
struct gl_texture_image;
typedef struct __GLcontextRec GLcontext;
typedef void (*FetchTexelFuncC) (const struct gl_texture_image * texImage,
				 GLint col, GLint row, GLint img,
				 GLchan * texelOut);
struct gl_texture_format
{
};
struct gl_texture_image
{
  GLenum _BaseFormat;
  GLboolean _IsPowerOfTwo;
  FetchTexelFuncC FetchTexelc;
};
struct gl_texture_object
{
  GLenum Target;
  GLenum WrapS;
  GLenum MinFilter;
  GLenum MagFilter;
  GLint BaseLevel;
  GLint _MaxLevel;
  struct gl_texture_image *Image[6][12];
};
enum _format
{
    MESA_FORMAT_RGBA_DXT3, MESA_FORMAT_RGBA_DXT5, MESA_FORMAT_RGBA,
    MESA_FORMAT_RGB, MESA_FORMAT_ALPHA, MESA_FORMAT_LUMINANCE,
};
typedef void (*texture_sample_func) (GLcontext * ctx,
				     const struct gl_texture_object * tObj,
				     GLuint n, const GLfloat texcoords[][4],
				     const GLfloat lambda[],
				     GLchan rgba[][4]);
lerp_2d (GLfloat a, GLfloat b, GLfloat v00, GLfloat v10, GLfloat v01,
	 GLfloat v11)
{
  const GLfloat temp0 = ((v00) + (a) * ((v10) - (v00)));
  const GLfloat temp1 = ((v01) + (a) * ((v11) - (v01)));
  return ((temp0) + (b) * ((temp1) - (temp0)));
}
static __inline__ void
lerp_rgba (GLchan result[4], GLfloat t, const GLchan a[4], const GLchan b[4])
{
  result[0] = (GLchan) (((a[0]) + (t) * ((b[0]) - (a[0]))) + 0.5);
  result[1] = (GLchan) (((a[1]) + (t) * ((b[1]) - (a[1]))) + 0.5);
  result[2] = (GLchan) (((a[2]) + (t) * ((b[2]) - (a[2]))) + 0.5);
}
static __inline__ void
lerp_rgba_2d (GLchan result[4], GLfloat a, GLfloat b, const GLchan t00[4],
	      const GLchan t10[4], const GLchan t01[4], const GLchan t11[4])
{
  result[0] = (GLchan) (lerp_2d (a, b, t00[0], t10[0], t01[0], t11[0]) + 0.5);
  result[1] = (GLchan) (lerp_2d (a, b, t00[1], t10[1], t01[1], t11[1]) + 0.5);
  result[2] = (GLchan) (lerp_2d (a, b, t00[2], t10[2], t01[2], t11[2]) + 0.5);
}
static __inline__ void
sample_2d_linear_repeat (GLcontext * ctx,
			 const struct gl_texture_object *tObj,
			 const struct gl_texture_image *img,
			 const GLfloat texcoord[4], GLchan rgba[])
{
  GLint i0, j0, i1, j1;
  GLfloat a, b;
  GLchan t00[4], t10[4], t01[4], t11[4];
  {
  };
  img->FetchTexelc (img, i1, j1, 0, t11);
  lerp_rgba_2d (rgba, a, b, t00, t10, t01, t11);
}
sample_2d_nearest_mipmap_linear (GLcontext * ctx,
				 const struct gl_texture_object *tObj,
				 GLuint n, const GLfloat texcoord[][4],
				 const GLfloat lambda[], GLchan rgba[][4])
{
  GLuint i;
  GLint level = linear_mipmap_level (tObj, lambda[i]);
  sample_2d_nearest (ctx, tObj, tObj->Image[0][tObj->_MaxLevel], texcoord[i], rgba[i]);
  GLchan t0[4], t1[4];
  sample_2d_nearest (ctx, tObj, tObj->Image[0][level], texcoord[i], t0);
  sample_2d_nearest (ctx, tObj, tObj->Image[0][level + 1], texcoord[i], t1);
}
static void
sample_2d_linear_mipmap_linear_repeat (GLcontext * ctx,
				       const struct gl_texture_object *tObj,
				       GLuint n, const GLfloat texcoord[][4],
				       const GLfloat lambda[],
				       GLchan rgba[][4])
{
  GLuint i;
  for (i = 0; i < n; i++)
    {
      GLint level = linear_mipmap_level (tObj, lambda[i]);
      if (level >= tObj->_MaxLevel)
	{
	  GLchan t0[4], t1[4];
	  const GLfloat f = ((lambda[i]) - ifloor (lambda[i]));
	  sample_2d_linear_repeat (ctx, tObj, tObj->Image[0][level],
				   texcoord[i], t0);
	  sample_2d_linear_repeat (ctx, tObj, tObj->Image[0][level + 1],
				   texcoord[i], t1);
	  lerp_rgba (rgba[i], f, t0, t1);
	}
    }
}
static void
sample_lambda_2d (GLcontext * ctx, const struct gl_texture_object *tObj,
		  GLuint n, const GLfloat texcoords[][4],
		  const GLfloat lambda[], GLchan rgba[][4])
{
  const struct gl_texture_image *tImg = tObj->Image[0][tObj->BaseLevel];
  GLuint minStart, minEnd;
  GLuint magStart, magEnd;
  const GLboolean repeatNoBorderPOT = (tObj->WrapS == 0x2901)
    && (tImg->_BaseFormat != 0x1900) && tImg->_IsPowerOfTwo;
  compute_min_mag_ranges (tObj, n, lambda, &minStart, &minEnd, &magStart,
			  &magEnd);
  if (minStart < minEnd)
    {
      const GLuint m = minEnd - minStart;
      switch (tObj->MinFilter)
	{
	case 0x2600:
	  if (repeatNoBorderPOT)
	    {
		case MESA_FORMAT_RGB:
		  opt_sample_rgb_2d (ctx, tObj, m, texcoords + minStart,
				     ((void *) 0), rgba + minStart);
		case MESA_FORMAT_RGBA:
		  opt_sample_rgba_2d (ctx, tObj, m, texcoords + minStart,
				      ((void *) 0), rgba + minStart);
	    }
	    {
	      sample_nearest_2d (ctx, tObj, m, texcoords + minStart,
				 ((void *) 0), rgba + minStart);
	    }
	  break;
	  sample_2d_nearest_mipmap_linear (ctx, tObj, m, texcoords + minStart,
					   lambda + minStart,
					   rgba + minStart);
	case 0x2703:
	  if (repeatNoBorderPOT)
	    sample_2d_linear_mipmap_linear_repeat (ctx, tObj, m,
						   texcoords + minStart,
						   lambda + minStart,
						   rgba + minStart);
	}
      switch (tObj->MagFilter)
	{
		case MESA_FORMAT_RGB:
		  opt_sample_rgb_2d (ctx, tObj, m, texcoords + magStart,
				     ((void *) 0), rgba + magStart);
		  opt_sample_rgba_2d (ctx, tObj, m, texcoords + magStart,
				      ((void *) 0), rgba + magStart);
                  sample_nearest_2d (ctx, tObj, m, texcoords + magStart,
                                     ((void *) 0), rgba + magStart);
	}
    }
}
texture_sample_func
_swrast_choose_texture_sample_func (const struct gl_texture_object *t)
{
      switch (t->Target)
	{
	case 0x0DE0:
	      return &sample_lambda_2d;
	}
}

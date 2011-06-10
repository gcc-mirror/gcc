/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */

typedef enum { GL_FALSE } GLenum;
typedef unsigned char GLboolean;
typedef int GLint;
typedef unsigned int GLuint;
typedef float GLfloat;
typedef double GLdouble;
typedef struct gl_context GLcontext;
struct gl_context {
  GLfloat TextureMatrix[16];
  GLenum Primitive;
};
void gl_GetDoublev( GLcontext *ctx, GLenum pname, GLdouble *params ) {
  GLuint i;
  for (i=0; i<16; i++)
    params[i] = (GLint) ctx->TextureMatrix[i];
}

/* { dg-final { cleanup-tree-dump "vect" } } */

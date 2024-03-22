/* Reduced from ImageMagick-7.1.0-57.  */

#define NULL ((void *)0)

typedef __builtin_va_list va_list;
typedef __SIZE_TYPE__ size_t;

typedef struct _ExceptionInfo ExceptionInfo;

void
ThrowMagickException(ExceptionInfo*,
		     const char*,
		     const char*,
		     ...) __attribute__((__format__(__printf__, 3, 4)));

typedef struct _Image
{
  /* [...snip...] */
  size_t columns, rows, depth, colors;
  /* [...snip...] */
} Image;

typedef struct _ImageInfo
{
  /* [...snip...] */
  char filename[4096];
  /* [...snip...] */
} ImageInfo;

extern Image *AcquireImage(const ImageInfo*, ExceptionInfo*);
extern void CloseBlob(Image*);
extern Image *DestroyImageList(Image*);

#define ThrowReaderException(tag) \
{ \
  (void) ThrowMagickException(exception, tag, \
    "`%s'",image_info->filename); \
  if ((image) != (Image *) NULL) \
    { \
      (void) CloseBlob(image); \
      image=DestroyImageList(image); \
    } \
  return((Image *) NULL); \
}

Image*
ReadMAPImage(const ImageInfo* image_info, ExceptionInfo* exception)
{
  Image* image;
  image = AcquireImage(image_info, exception);
  if ((image->columns == 0) || (image->rows == 0))
    ThrowReaderException("MustSpecifyImageSize");
  return image;
}

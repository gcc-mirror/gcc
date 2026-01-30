// check bugs in importing C files

int squared(int a)
{
    return a * a;
}

/* test case for issue #21094 */
typedef enum upng_error {
    UPNG_EOK            = 0, /* success (no error) */
    UPNG_ENOMEM         = 1, /* memory allocation failed */
    UPNG_ENOTFOUND      = 2, /* resource not found (file missing) */
    UPNG_ENOTPNG        = 3, /* image data does not have a PNG header */
    UPNG_EMALFORMED     = 4, /* image data is not a valid PNG image */
    UPNG_EUNSUPPORTED   = 5, /* critical PNG chunk type is not supported */
    UPNG_EUNINTERLACED  = 6, /* image interlacing is not supported */
    UPNG_EUNFORMAT      = 7, /* image color format is not supported */
    UPNG_EPARAM         = 8  /* invalid parameter to method call */
} upng_error;

typedef int __attribute__ ((const)) (*x264_pixel_cmp_t)(void);

typedef struct {
    x264_pixel_cmp_t ssd;
} x264_pixel_function_t;

int x264_pixel_ssd_wxh (x264_pixel_function_t *pf, int i_width) {
    int i_ssd = 0, x;
    for (x = 0; x < i_width; x++)
      i_ssd += pf->ssd();
    return i_ssd;
}

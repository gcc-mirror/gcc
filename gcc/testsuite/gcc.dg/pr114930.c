/* PR c/114930 */
/* { dg-do compile { target lto } } */
/* { dg-options "-std=c23 -flto" } */

typedef struct WebPPicture WebPPicture;
typedef int (*WebPProgressHook)(const WebPPicture *);
WebPProgressHook progress_hook;
struct WebPPicture {
} WebPGetColorPalette(const struct WebPPicture *);

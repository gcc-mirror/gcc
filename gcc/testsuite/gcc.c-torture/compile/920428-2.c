double sin(double x);
double cos(double x);
double tan(double x);
double asin(double x);
double acos(double x);
double atan(double x);
double atan2(double y, double x);
double sinh(double x);
double cosh(double x);
double tanh(double x);
double exp(double x);
double expm1(double x);
double log(double x);
double log10(double x);
double log1p(double x);
double pow(double x, double y);
double sqrt(double x);
double cbrt(double x);
double ceil(double x);
double floor(double x);
double fabs(double x);
double frexp(double value, int *eptr);
double ldexp(double value, int exp);
double modf(double value, double *iptr);
double erf(double x);
double erfc(double x);
double atof(const char *nptr);
double hypot(double x, double y);
double lgamma(double x);
double j0(double x);
double j1(double x);
double jn(int n, double x);
double y0(double x);
double y1(double x);
double yn(int n, double x);
extern struct _iobuf {
 int _cnt;
 char *_ptr;
 char *_base;
 int _bufsiz;
 short _flag;
 char _file;
} _iob[];
typedef unsigned long size_t;
typedef char *va_list;
struct _iobuf *fopen(const char *filename, const char *type);
struct _iobuf *freopen(const char *filename, const char *type, struct _iobuf *stream);
struct _iobuf *fdopen(int fildes, const char *type);
struct _iobuf *popen(const char *command, const char *type);
int pclose(struct _iobuf *stream);
int fflush(struct _iobuf *stream);
int fclose(struct _iobuf *stream);
int remove(const char *path);
int rename(const char *from, const char *to);
struct _iobuf *tmpfile(void);
char *tmpnam(char *s);
int setvbuf(struct _iobuf *iop, char *buf, int type, size_t size);
int setbuf(struct _iobuf *stream, char *buf);
int setbuffer(struct _iobuf *stream, char *buf, size_t size);
int setlinebuf(struct _iobuf *stream);
int fprintf(struct _iobuf *stream, const char *format, ...);
int printf(const char *format, ...);
char *sprintf(char *s, const char *format, ...);
int vfprintf(struct _iobuf *stream, const char *format, va_list arg);
int vprintf(const char *format, va_list arg);
int vsprintf(char *s, const char *format, va_list arg);
int fscanf(struct _iobuf *stream, const char *format, ...);
int scanf(const char *format, ...);
int sscanf(char *s, const char *format, ...);
int fgetc(struct _iobuf *stream);
int getw(struct _iobuf *stream);
char *fgets(char *s, int n, struct _iobuf *stream);
char *gets(char *s);
int fputc(int c, struct _iobuf *stream);
int putw(int w, struct _iobuf *stream);
int fputs(const char *s, struct _iobuf *stream);
int puts(const char *s);
int ungetc(int c, struct _iobuf *stream);
int fread(void *ptr, size_t size, size_t count, struct _iobuf *iop);
int fwrite(const void *ptr, size_t size, size_t count, struct _iobuf *iop);
int fseek(struct _iobuf *stream, long offset, int ptrname);
long ftell(struct _iobuf *stream);
void rewind(struct _iobuf *stream);
int fgetpos(struct _iobuf *stream, long *pos);
int fsetpos(struct _iobuf *stream, const long *pos);
void perror(const char *s);
typedef unsigned char byte;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
typedef unsigned short ushort_;
typedef struct _physadr { int r[1]; } *physadr;
typedef struct label_t {
 int val[11];
} label_t;
typedef struct _quad { long val[2]; } quad;
typedef long daddr_t;
typedef char * caddr_t;
typedef u_long ino_t;
typedef long swblk_t;
typedef long time_t;
typedef short dev_t;
typedef long off_t;
typedef u_short uid_t;
typedef u_short gid_t;
typedef signed char prio_t;
typedef long fd_mask;
typedef struct fd_set {
 fd_mask fds_bits[(((256 )+(( (sizeof(fd_mask) * 8 ) )-1))/( (sizeof(fd_mask) * 8 ) )) ];
} fd_set;
typedef struct qhdr {
 struct qhdr *link, *rlink;
} *queue_t;
typedef char *ptr_ord_t;
typedef double floatp;
typedef char *(*proc_alloc_t)(unsigned num_elements, unsigned element_size, const char *client_name );
typedef void (*proc_free_t)(char *data, unsigned num_elements, unsigned element_size, const char *client_name );
extern struct _iobuf *gs_out;
typedef struct gs_point_s {
 double x, y;
} gs_point;
typedef struct gs_int_point_s {
 int x, y;
} gs_int_point;
typedef struct gs_rect_s {
 gs_point p, q;
} gs_rect;
typedef struct gs_int_rect_s {
 gs_int_point p, q;
} gs_int_rect;
typedef struct gs_state_s gs_state;
typedef struct {
 proc_alloc_t alloc;
 proc_free_t free;
} gs_memory_procs;
char *gs_malloc(uint, uint, const char * );
void gs_free(char *, uint, uint, const char * );
extern char gs_debug[128];
extern int gs_log_error(int, const char *, int );
typedef long fixed;
typedef struct gs_fixed_point_s {
 fixed x, y;
} gs_fixed_point;
typedef struct gs_fixed_rect_s {
 gs_fixed_point p, q;
} gs_fixed_rect;
typedef struct gs_matrix_s {
 long _xx; float xx; long _xy; float xy; long _yx; float yx; long _yy; float yy; long _tx; float tx; long _ty; float ty;
} gs_matrix;
void gs_make_identity(gs_matrix * );
int gs_make_translation(floatp, floatp, gs_matrix * ),
 gs_make_scaling(floatp, floatp, gs_matrix * ),
 gs_make_rotation(floatp, gs_matrix * );
int gs_matrix_multiply(const gs_matrix *, const gs_matrix *, gs_matrix * ),
 gs_matrix_invert(const gs_matrix *, gs_matrix * ),
 gs_matrix_rotate(const gs_matrix *, floatp, gs_matrix * );
int gs_point_transform(floatp, floatp, const gs_matrix *, gs_point * ),
 gs_point_transform_inverse(floatp, floatp, const gs_matrix *, gs_point * ),
 gs_distance_transform(floatp, floatp, const gs_matrix *, gs_point * ),
 gs_distance_transform_inverse(floatp, floatp, const gs_matrix *, gs_point * ),
 gs_bbox_transform_inverse(gs_rect *, gs_matrix *, gs_rect * );
typedef struct gs_matrix_fixed_s {
 long _xx; float xx; long _xy; float xy; long _yx; float yx; long _yy; float yy; long _tx; float tx; long _ty; float ty;
 fixed tx_fixed, ty_fixed;
} gs_matrix_fixed;
extern void gs_update_matrix_fixed(gs_matrix_fixed * );
int gs_point_transform2fixed(gs_matrix_fixed *, floatp, floatp, gs_fixed_point * ),
 gs_distance_transform2fixed(gs_matrix_fixed *, floatp, floatp, gs_fixed_point * );
typedef struct {
 long xx, xy, yx, yy;
 int skewed;
 int shift;
 int max_bits;
 fixed round;
} fixed_coeff;

typedef enum {
 gs_cap_butt = 0,
 gs_cap_round = 1,
 gs_cap_square = 2
} gs_line_cap;
typedef enum {
 gs_join_miter = 0,
 gs_join_round = 1,
 gs_join_bevel = 2
} gs_line_join;
gs_state *gs_state_alloc(proc_alloc_t, proc_free_t );
int gs_state_free(gs_state * );
int gs_gsave(gs_state * ),
 gs_grestore(gs_state * ),
 gs_grestoreall(gs_state * );
gs_state *gs_gstate(gs_state * );
int gs_currentgstate(gs_state * , const gs_state * ),
 gs_setgstate(gs_state * , const gs_state * );
gs_state *gs_state_swap_saved(gs_state *, gs_state * );
void gs_state_swap(gs_state *, gs_state * );
int gs_initgraphics(gs_state * );
typedef struct gx_device_s gx_device;
int gs_flushpage(gs_state * );
int gs_copypage(gs_state * );
int gs_output_page(gs_state *, int, int );
int gs_copyscanlines(gx_device *, int, byte *, uint, int *, uint * );
gx_device * gs_getdevice(int );
int gs_copydevice(gx_device **, gx_device *, proc_alloc_t );
int gs_makeimagedevice(gx_device **, gs_matrix *, uint, uint, byte *, int, proc_alloc_t );
void gs_nulldevice(gs_state * );
int gs_setdevice(gs_state *, gx_device * );
gx_device * gs_currentdevice(gs_state * );
const char * gs_devicename(gx_device * );
void gs_deviceinitialmatrix(gx_device *, gs_matrix * );
int gs_closedevice(gx_device * );
int gs_setlinewidth(gs_state *, floatp );
float gs_currentlinewidth(const gs_state * );
int gs_setlinecap(gs_state *, gs_line_cap );
gs_line_cap gs_currentlinecap(const gs_state * );
int gs_setlinejoin(gs_state *, gs_line_join );
gs_line_join gs_currentlinejoin(const gs_state * );
int gs_setmiterlimit(gs_state *, floatp );
float gs_currentmiterlimit(const gs_state * );
int gs_setdash(gs_state *, const float *, uint, floatp );
uint gs_currentdash_length(const gs_state * );
int gs_currentdash_pattern(const gs_state *, float * );
float gs_currentdash_offset(const gs_state * );
int gs_setflat(gs_state *, floatp );
float gs_currentflat(const gs_state * );
int gs_setstrokeadjust(gs_state *, int );
int gs_currentstrokeadjust(const gs_state * );
typedef enum {
 gs_color_space_DeviceGray = 0,
 gs_color_space_DeviceRGB,
 gs_color_space_DeviceCMYK
} gs_color_space;
typedef struct gs_color_s gs_color;
extern const uint gs_color_sizeof;
int gs_setgray(gs_state *, floatp );
float gs_currentgray(gs_state * );
int gs_sethsbcolor(gs_state *, floatp, floatp, floatp ),
 gs_currenthsbcolor(gs_state *, float [3] ),
 gs_setrgbcolor(gs_state *, floatp, floatp, floatp ),
 gs_currentrgbcolor(gs_state *, float [3] );
int gs_currentcolorspace(gs_state *, gs_color_space * );
typedef float (*gs_transfer_proc)(gs_state *, floatp );
int gs_settransfer(gs_state *, gs_transfer_proc ),
 gs_settransfer_remap(gs_state *, gs_transfer_proc, int );
gs_transfer_proc gs_currenttransfer(gs_state * );
int gs_setcolortransfer(gs_state *, gs_transfer_proc ,
 gs_transfer_proc , gs_transfer_proc ,
 gs_transfer_proc ),
 gs_setcolortransfer_remap(gs_state *, gs_transfer_proc ,
 gs_transfer_proc , gs_transfer_proc ,
 gs_transfer_proc , int );
void gs_currentcolortransfer(gs_state *, gs_transfer_proc [4] );
int gs_setscreen(gs_state *, floatp, floatp, float (*)(floatp, floatp ) );
int gs_currentscreen(gs_state *, float *, float *, float (**)(floatp, floatp ) );
int gs_sethalftonephase(gs_state *, int, int );
int gs_currenthalftonephase(gs_state *, gs_int_point * );
typedef struct gs_screen_enum_s gs_screen_enum;
extern const uint gs_screen_enum_sizeof;
int gs_screen_init(gs_screen_enum *, gs_state *, floatp, floatp );
int gs_screen_currentpoint(gs_screen_enum *, gs_point * );
int gs_screen_next(gs_screen_enum *, floatp );
struct gs_state_s {
 gs_state *saved;
 gs_memory_procs memory_procs;
 gs_matrix_fixed ctm;
 gs_matrix ctm_inverse;
 int inverse_valid;
 struct gx_path_s *path;
 struct gx_clip_path_s *clip_path;
 int clip_rule;
 struct line_params_s *line_params;
 struct halftone_params_s *halftone;
 float (*ht_proc)(floatp, floatp );
 gs_int_point ht_phase;
 gs_int_point phase_mod;
 struct gs_color_s *color;
 struct gx_device_color_s *dev_color;
 struct gx_transfer_s *transfer;
 struct gs_font_s *font;
 gs_matrix char_tm;
 int char_tm_valid;
 byte in_cachedevice;
 byte in_charpath;




 int level;
 float flatness;
 int stroke_adjust;
 struct device_s *device;
 int device_is_shared;

};
typedef unsigned long gx_bitmap_id;
typedef struct gx_bitmap_s {
 byte *data;
 int raster;
 gs_int_point size;
 gx_bitmap_id id;
 ushort rep_width, rep_height;
} gx_bitmap;
typedef unsigned long gx_color_index;
typedef unsigned short gx_color_value;
typedef struct gx_device_color_info_s {
 int num_components;

 int depth;
 gx_color_value max_gray;
 gx_color_value max_rgb;

 gx_color_value dither_gray;
 gx_color_value dither_rgb;

} gx_device_color_info;
typedef struct gx_device_procs_s gx_device_procs;
struct gx_device_s {
 int params_size; gx_device_procs *procs; const char *dname; int width; int height; float x_pixels_per_inch; float y_pixels_per_inch; float l_margin, b_margin, r_margin, t_margin; gx_device_color_info color_info; int is_open;
};
typedef struct gs_prop_item_s gs_prop_item;
struct gx_device_procs_s {
 int (*open_device)(gx_device *dev );
 void (*get_initial_matrix)(gx_device *dev, gs_matrix *pmat );
 int (*sync_output)(gx_device *dev );
 int (*output_page)(gx_device *dev, int num_copies, int flush );
 int (*close_device)(gx_device *dev );
 gx_color_index (*map_rgb_color)(gx_device *dev, gx_color_value red, gx_color_value green, gx_color_value blue );
 int (*map_color_rgb)(gx_device *dev, gx_color_index color, gx_color_value rgb[3] );
 int (*fill_rectangle)(gx_device *dev, int x, int y, int width, int height, gx_color_index color );
 int (*tile_rectangle)(gx_device *dev, gx_bitmap *tile, int x, int y, int width, int height, gx_color_index color0, gx_color_index color1, int phase_x, int phase_y );
 int (*copy_mono)(gx_device *dev, unsigned char *data, int data_x, int raster, gx_bitmap_id id, int x, int y, int width, int height, gx_color_index color0, gx_color_index color1 );
 int (*copy_color)(gx_device *dev, unsigned char *data, int data_x, int raster, gx_bitmap_id id, int x, int y, int width, int height );
 int (*draw_line)(gx_device *dev, int x0, int y0, int x1, int y1, gx_color_index color );
 int (*get_bits)(gx_device *dev, int y, unsigned char *data, unsigned int size, int pad_to_word );
 int (*get_props)(gx_device *dev, gs_prop_item *plist );

 int (*put_props)(gx_device *dev, gs_prop_item *plist, int count );

};
extern unsigned int gx_device_bytes_per_scan_line(gx_device *dev, int pad_to_word );
int gx_default_open_device(gx_device *dev );
void gx_default_get_initial_matrix(gx_device *dev, gs_matrix *pmat );
int gx_default_sync_output(gx_device *dev );
int gx_default_output_page(gx_device *dev, int num_copies, int flush );
int gx_default_close_device(gx_device *dev );
gx_color_index gx_default_map_rgb_color(gx_device *dev, gx_color_value red, gx_color_value green, gx_color_value blue );
int gx_default_map_color_rgb(gx_device *dev, gx_color_index color, gx_color_value rgb[3] );
int gx_default_tile_rectangle(gx_device *dev, gx_bitmap *tile, int x, int y, int width, int height, gx_color_index color0, gx_color_index color1, int phase_x, int phase_y );
int gx_default_copy_color(gx_device *dev, unsigned char *data, int data_x, int raster, gx_bitmap_id id, int x, int y, int width, int height );
int gx_default_draw_line(gx_device *dev, int x0, int y0, int x1, int y1, gx_color_index color );
int gx_default_get_bits(gx_device *dev, int y, unsigned char *data, unsigned int size, int pad_to_word );
int gx_default_get_props(gx_device *dev, gs_prop_item *plist );
int gx_default_put_props(gx_device *dev, gs_prop_item *plist, int count );
typedef struct device_s {
 gx_device *info;
 int is_band_device;
 gx_color_index white, black;
} device;
int gs_initmatrix(gs_state * ),
 gs_defaultmatrix(const gs_state *, gs_matrix * ),
 gs_currentmatrix(const gs_state *, gs_matrix * ),
 gs_setmatrix(gs_state *, const gs_matrix * ),
 gs_translate(gs_state *, floatp, floatp ),
 gs_scale(gs_state *, floatp, floatp ),
 gs_rotate(gs_state *, floatp ),
 gs_concat(gs_state *, const gs_matrix * );
int gs_transform(gs_state *, floatp, floatp, gs_point * ),
 gs_dtransform(gs_state *, floatp, floatp, gs_point * ),
 gs_itransform(gs_state *, floatp, floatp, gs_point * ),
 gs_idtransform(gs_state *, floatp, floatp, gs_point * );
static int
ctm_set_inverse(gs_state *pgs)
{ int code = gs_matrix_invert(&*(gs_matrix *)&(pgs)->ctm , &pgs->ctm_inverse);
 0;
 if ( code < 0 ) return code;
 pgs->inverse_valid = 1;
 return 0;
}
void
gs_update_matrix_fixed(gs_matrix_fixed *pmat)
{ (*pmat). tx = ((float)(((*pmat). tx_fixed = ((fixed)(((*pmat). tx)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , (*pmat). ty = ((float)(((*pmat). ty_fixed = ((fixed)(((*pmat). ty)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) )));
}
int
gs_initmatrix(gs_state *pgs)
{ gx_device *dev = pgs->device->info;
 (*dev->procs->get_initial_matrix)(dev, &*(gs_matrix *)&(pgs)->ctm );
 (pgs->ctm). tx = ((float)(((pgs->ctm). tx_fixed = ((fixed)(((pgs->ctm). tx)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , (pgs->ctm). ty = ((float)(((pgs->ctm). ty_fixed = ((fixed)(((pgs->ctm). ty)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
 return 0;
}
int
gs_defaultmatrix(const gs_state *pgs, gs_matrix *pmat)
{ gx_device *dev = pgs->device->info;
 (*dev->procs->get_initial_matrix)(dev, pmat);
 return 0;
}
int
gs_currentmatrix(const gs_state *pgs, gs_matrix *pmat)
{ *pmat = *(gs_matrix *)&(pgs)->ctm;
 return 0;
}
int
gs_setmatrix(gs_state *pgs, const gs_matrix *pmat)
{ *(gs_matrix *)&(pgs)->ctm = *pmat;
 (pgs->ctm). tx = ((float)(((pgs->ctm). tx_fixed = ((fixed)(((pgs->ctm). tx)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , (pgs->ctm). ty = ((float)(((pgs->ctm). ty_fixed = ((fixed)(((pgs->ctm). ty)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
 return 0;
}
int
gs_translate(gs_state *pgs, floatp dx, floatp dy)
{ gs_point pt;
 int code;
 if ( (code = gs_distance_transform(dx, dy, &*(gs_matrix *)&(pgs)->ctm , &pt)) < 0 )
 return code;
 pgs->ctm.tx += pt.x;
 pgs->ctm.ty += pt.y;
 (pgs->ctm). tx = ((float)(((pgs->ctm). tx_fixed = ((fixed)(((pgs->ctm). tx)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , (pgs->ctm). ty = ((float)(((pgs->ctm). ty_fixed = ((fixed)(((pgs->ctm). ty)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
 return 0;
}
int
gs_scale(gs_state *pgs, floatp sx, floatp sy)
{ pgs->ctm.xx *= sx;
 pgs->ctm.xy *= sx;
 pgs->ctm.yx *= sy;
 pgs->ctm.yy *= sy;
 pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
 return 0;
}
int
gs_rotate(gs_state *pgs, floatp ang)
{ int code = gs_matrix_rotate(&*(gs_matrix *)&(pgs)->ctm , ang, &*(gs_matrix *)&(pgs)->ctm );
 pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
 return code;
}
int
gs_concat(gs_state *pgs, const gs_matrix *pmat)
{ int code = gs_matrix_multiply(pmat, &*(gs_matrix *)&(pgs)->ctm , &*(gs_matrix *)&(pgs)->ctm );
 (pgs->ctm). tx = ((float)(((pgs->ctm). tx_fixed = ((fixed)(((pgs->ctm). tx)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , (pgs->ctm). ty = ((float)(((pgs->ctm). ty_fixed = ((fixed)(((pgs->ctm). ty)*(float)(1<<12 ) )) )*(1.0/(1<<12 ) ))) , pgs->inverse_valid = 0, pgs->char_tm_valid = 0;
 return code;
}
int
gs_transform(gs_state *pgs, floatp x, floatp y, gs_point *pt)
{ return gs_point_transform(x, y, &*(gs_matrix *)&(pgs)->ctm , pt);
}
int
gs_dtransform(gs_state *pgs, floatp dx, floatp dy, gs_point *pt)
{ return gs_distance_transform(dx, dy, &*(gs_matrix *)&(pgs)->ctm , pt);
}
int
gs_itransform(gs_state *pgs, floatp x, floatp y, gs_point *pt)
{

 if ( !!(((*(long *)(&((&pgs->ctm)->xy)) | *(long *)(&( (&pgs->ctm)->yx)) ) << 1) == 0) )
 { return gs_point_transform_inverse(x, y, &*(gs_matrix *)&(pgs)->ctm , pt);
 }
 else
 { if ( !pgs->inverse_valid ) { int code = ctm_set_inverse(pgs); if ( code < 0 ) return code; };
 return gs_point_transform(x, y, &pgs->ctm_inverse, pt);
 }
}
int
gs_idtransform(gs_state *pgs, floatp dx, floatp dy, gs_point *pt)
{

 if ( !!(((*(long *)(&((&pgs->ctm)->xy)) | *(long *)(&( (&pgs->ctm)->yx)) ) << 1) == 0) )
 { return gs_distance_transform_inverse(dx, dy,
 &*(gs_matrix *)&(pgs)->ctm , pt);
 }
 else
 { if ( !pgs->inverse_valid ) { int code = ctm_set_inverse(pgs); if ( code < 0 ) return code; };
 return gs_distance_transform(dx, dy, &pgs->ctm_inverse, pt);
 }
}
int
gs_translate_to_fixed(register gs_state *pgs, fixed px, fixed py)
{ pgs->ctm.tx = ((float)((pgs->ctm.tx_fixed = px)*(1.0/(1<<12 ) )));
 pgs->ctm.ty = ((float)((pgs->ctm.ty_fixed = py)*(1.0/(1<<12 ) )));
 pgs->inverse_valid = 0;
 pgs->char_tm_valid = 1;
 return 0;
}
int
gx_matrix_to_fixed_coeff(const gs_matrix *pmat, register fixed_coeff *pfc,
 int max_bits)
{ gs_matrix ctm;
 int scale = -10000;
 int expt, shift;
 ctm = *pmat;
 pfc->skewed = 0;
 if ( !((*(long *)(&(ctm.xx)) << 1) == 0) )
 { (void)frexp(ctm.xx, &scale);
 }
 if ( !((*(long *)(&(ctm.xy)) << 1) == 0) )
 { (void)frexp(ctm.xy, &expt);
 if ( expt > scale ) scale = expt;
 pfc->skewed = 1;
 }
 if ( !((*(long *)(&(ctm.yx)) << 1) == 0) )
 { (void)frexp(ctm.yx, &expt);
 if ( expt > scale ) scale = expt;
 pfc->skewed = 1;
 }
 if ( !((*(long *)(&(ctm.yy)) << 1) == 0) )
 { (void)frexp(ctm.yy, &expt);
 if ( expt > scale ) scale = expt;
 }
 scale = sizeof(long) * 8 - 1 - max_bits - scale;
 shift = scale - 12;
 if ( shift > 0 )
 { pfc->shift = shift;
 pfc->round = (fixed)1 << (shift - 1);
 }
 else
 { pfc->shift = 0;
 pfc->round = 0;
 scale -= shift;
 }
 pfc->xx = (((*(long *)(&(ctm.xx)) << 1) == 0) ? 0 : (long)ldexp(ctm.xx, scale));
 pfc->yy = (((*(long *)(&(ctm.yy)) << 1) == 0) ? 0 : (long)ldexp(ctm.yy, scale));
 if ( pfc->skewed )
 { pfc->xy = (((*(long *)(&(ctm.xy)) << 1) == 0) ? 0 : (long)ldexp(ctm.xy, scale));
 pfc->yx = (((*(long *)(&(ctm.yx)) << 1) == 0) ? 0 : (long)ldexp(ctm.yx, scale));
 }
 else
 pfc->xy = pfc->yx = 0;
 pfc->max_bits = max_bits;
 return 0;
}

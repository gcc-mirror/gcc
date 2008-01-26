typedef unsigned int size_t;
         typedef struct {
        }
         HashTable;
         typedef struct _zval_struct zval;
         typedef struct _zend_guard {
         HashTable *ht;
        }
         zvalue_value;
         struct _zval_struct {
         zvalue_value value;
        }
         php_output_globals;
         typedef struct _php_stream php_stream;
         typedef struct _php_stream_filter php_stream_filter;
         typedef struct _php_stream_bucket_brigade php_stream_bucket_brigade;
         typedef enum {
         PSFS_ERR_FATAL,  PSFS_FEED_ME,  PSFS_PASS_ON, }
         php_stream_filter_status_t;
         typedef struct _php_stream_filter_ops {
         php_stream_filter_status_t (*filter)(    php_stream *stream,    php_stream_filter *thisfilter,    php_stream_bucket_brigade *buckets_in,    php_stream_bucket_brigade *buckets_out,    size_t *bytes_consumed,    int flags    );
         void (*dtor)(php_stream_filter *thisfilter );
         const char *label;
        }
         php_stream_filter_ops;
         struct _php_stream_filter {
        };
         typedef struct _php_stream_filter_factory {
         php_stream_filter *(*create_filter)(const char *filtername, zval *filterparams, int persistent );
        }
         php_stream_filter_factory;
         typedef enum _php_conv_err_t {
         PHP_CONV_ERR_SUCCESS = 0,  PHP_CONV_ERR_UNKNOWN,  PHP_CONV_ERR_TOO_BIG,  PHP_CONV_ERR_INVALID_SEQ,  PHP_CONV_ERR_UNEXPECTED_EOS,  PHP_CONV_ERR_EXISTS,  PHP_CONV_ERR_MORE,  PHP_CONV_ERR_ALLOC,  PHP_CONV_ERR_NOT_FOUND }
         php_conv_err_t;
         typedef struct _php_conv php_conv;
         typedef php_conv_err_t (*php_conv_convert_func)(php_conv *, const char **, size_t *, char **, size_t *);
         struct _php_conv {
         php_conv_convert_func convert_op;
        }
         php_conv_base64_decode;
         typedef struct _php_conv_qprint_decode {
         php_conv _super;
         const char *lbchars;
        }
         php_conv_qprint_decode;
         static php_conv_err_t php_conv_qprint_decode_convert(php_conv_qprint_decode *inst, const char **in_pp, size_t *in_left_p, char **out_pp, size_t *out_left_p) {
         size_t icnt, ocnt;
         unsigned char *ps, *pd;
         unsigned int scan_stat;
         unsigned int lb_ptr, lb_cnt;
         for (;
       ;
       ) {
         switch (scan_stat) {
         case 0: {
         if (*ps == '=') {
         scan_stat = 1;
        }
     else {
         if (ocnt < 1) {
         goto out;
        }
         *(pd++) = *ps;
         ocnt--;
        }
        }
      break;
         case 1: {
         if (*ps == ' ' || *ps == '\t') {
        }
     else if (!inst->lbchars && lb_cnt == 0 && *ps == '\r') {
         lb_cnt++;
         scan_stat = 5;
         break;
        }
     else if (!inst->lbchars && lb_cnt == 0 && *ps == '\n') {
         scan_stat = 0;
         break;
        }
        }
         case 2: {
         if (icnt <= 0) {
         goto out;
        }
        }
         case 3: {
        }
         case 4: {
         ps++, icnt--;
        }
        }
        }
        out:  *in_pp = (const char *)ps;
        }
         static php_conv_err_t php_conv_qprint_decode_ctor(php_conv_qprint_decode *inst, const char *lbchars, size_t lbchars_len, int lbchars_dup, int persistent) {
         inst->_super.convert_op = (php_conv_convert_func) php_conv_qprint_decode_convert;
        }
         typedef struct _php_convert_filter {
         php_conv *cd;
        }
         php_convert_filter;
         static php_conv *php_conv_open(int conv_mode, const HashTable *options, int persistent) {
         php_conv *retval = ((void *)0);
         switch (conv_mode) {
         case 4: {
         char *lbchars = ((void *)0);
         size_t lbchars_len;
         if (lbchars != ((void *)0)) {
         if (php_conv_qprint_decode_ctor((php_conv_qprint_decode *)retval, lbchars, lbchars_len, 1, persistent)) {
        }
        }
        }
        }
        }
         static int php_convert_filter_ctor(php_convert_filter *inst,  int conv_mode, HashTable *conv_opts,  const char *filtername, int persistent) {
         if ((inst->cd = php_conv_open(conv_mode, conv_opts, persistent)) == ((void *)0)) {
        }
        }
         static php_stream_filter_status_t strfilter_convert_filter(  php_stream *stream,  php_stream_filter *thisfilter,  php_stream_bucket_brigade *buckets_in,  php_stream_bucket_brigade *buckets_out,  size_t *bytes_consumed,  int flags  ) {
        }
         static void strfilter_convert_dtor(php_stream_filter *thisfilter ) {
        }
         static php_stream_filter_ops strfilter_convert_ops = {
         strfilter_convert_filter,  strfilter_convert_dtor,  "convert.*" };
         static php_stream_filter *strfilter_convert_create(const char *filtername, zval *filterparams, int persistent ) {
         php_convert_filter *inst;
         int conv_mode = 0;
         if (php_convert_filter_ctor(inst, conv_mode,   (filterparams != ((void *)0) ? (*filterparams).value.ht : ((void *)0)),   filtername, persistent) != 0) {
        }
        }
         static php_stream_filter_factory strfilter_convert_factory = {
         strfilter_convert_create };
         static const struct {
         php_stream_filter_ops *ops;
         php_stream_filter_factory *factory;
        }
         standard_filters[] = {
         {
       &strfilter_convert_ops, &strfilter_convert_factory }
        };
         int zm_startup_standard_filters(int type, int module_number ) {
         int i;
         for (i = 0;
        standard_filters[i].ops;
        i++) {
        }
        }

struct ist {
  char ptr;
  long len;
} __trans_tmp_1, http_update_host_authority;
int http_update_host_sl_0_0_0;
void http_update_host(const struct ist uri) {
  uri.len || uri.ptr;
  if (http_update_host_sl_0_0_0) {
    http_update_host_authority = __trans_tmp_1;
    !http_update_host_authority.len;
  } else
    http_update_host_authority = uri;
}

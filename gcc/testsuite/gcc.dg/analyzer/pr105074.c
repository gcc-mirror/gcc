/* { dg-additional-options "-O2 -fdump-analyzer-untracked" } */

void _gnutls_log(const char *);
static void _gnutls_ocsp_verify_mandatory_stapling(void) {
  _gnutls_log(__func__); /* { dg-warning "track '__func__': no" } */
}
void check_ocsp_response_gnutls_x509_cert_verify_peers(void) {
  _gnutls_ocsp_verify_mandatory_stapling();
}

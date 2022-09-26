/* { dg-do compile } */
/* { dg-options "-O -fexceptions -Wuninitialized" } */

void l_free (void *);
char *l_settings_get_string ();
void eap_append_secret ();
inline void auto_free(void *a) {
  void **p = a;
  l_free(*p); /* { dg-warning "uninitialized" } */
}
void eap_gtc_check_settings() {
  char *identity __attribute__((cleanup(auto_free)));
  char password __attribute__((cleanup(auto_free)));
  identity = l_settings_get_string();
  eap_append_secret();
}

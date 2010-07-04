/* { dg-do compile } */
/* { dg-options "-Werror=implicit" } */
int main() {
  return pippo(); /* { dg-error "" } */
}
/* { dg-message "warnings being treated as errors" "" { target *-*-* } 0 } */

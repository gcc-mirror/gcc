#define ugly 3
#ugly "foobar" 3	/* { dg-error "invalid" "invalid directive" } */
main() { exit (0); }

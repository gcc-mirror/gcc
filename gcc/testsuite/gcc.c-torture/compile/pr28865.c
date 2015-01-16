struct var_len
{
  int field1;
  const char field2[];
};

/* Note - strictly speaking this array declaration is illegal
   since each element has a variable length.  We used to allow
   this because it was used in existing code.
   Since PR64417 we reject this code.  */
static const struct var_len var_array[] = 
{
  { 1, "Long exposure noise reduction" }, /* { dg-error "initialization of flexible array member" } */
  { 2, "Shutter/AE lock buttons" }, /* { dg-error "initialization of flexible array member" } */
  { 3, "Mirror lockup" } /* { dg-error "initialization of flexible array member" } */
};

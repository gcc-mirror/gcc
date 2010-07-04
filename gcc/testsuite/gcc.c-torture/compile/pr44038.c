struct Ustr {
    char data[1]; 
};
int ustr_xi__embed_val_get(char *);
inline static int ustr_len(struct Ustr *s1)
{
  return ustr_xi__embed_val_get(s1->data);
}
static struct Ustr *s1 = ((struct Ustr *) "");
int tst(char *cstr)
{
  return ustr_len(s1);
}

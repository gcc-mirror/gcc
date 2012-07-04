/* { dg-do compile } */

typedef struct s {
    int value;
} s_t;

static inline int 
read(s_t const *var)
{
  return var->value;
}

int main()
{
  extern union u extern_var;
  return read((s_t *)&extern_var);
}

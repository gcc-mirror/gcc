// { dg-do assemble  }
// Origin: bkoz@nabi.net

typedef struct
{
  int count;
} mbstate_t;

struct fpos
{
  mbstate_t  _M_st;
  fpos(int __pos)
    : _M_st() { 
  }
};

int main ()
{
  fpos f (2);
}



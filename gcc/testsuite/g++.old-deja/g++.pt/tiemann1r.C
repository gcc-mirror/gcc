// Build don't link: 

struct bs_1
{
  typedef int (*p_user_hashf)(int);
};

bs_1::p_user_hashf i_user_hashf;

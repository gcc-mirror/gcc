typedef int int32_t;
typedef unsigned int uint32_t;
int safe_mod_u_u (unsigned int, unsigned int);
static inline int
safe_add_s_s (int si1, int si2)
{
  if ((si1 > 0) && (si2 > 0) && (si1 > (si2)) || (si1 < 0) && (si2 < 0)
      && (si1 < ((-__INT_MAX__ - 1) - si2)))
    return si1;
}

uint32_t g_8;
uint32_t
func_24 (int32_t p_25)
{
  uint32_t l_30 = -1L;
  if ((safe_mod_u_u (1, 1)) | (safe_add_s_s (g_8, l_30)))
    return 1;
}


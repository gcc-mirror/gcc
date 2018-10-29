# define DBG_ERROR(dbg_logger, format, args...) if (1){\
  char dbg_buffer[256]; \
  __builtin_snprintf(dbg_buffer, sizeof(dbg_buffer)-1,\
  __FILE__":%5d: " format  , __LINE__ , ## args); \
};

void testPasswordStore1(int argc, char **argv) {
  const char *pw1="Secret1";
  char pw[256];
  DBG_ERROR(0, "Bad password, expected [%s], got [%s].", pw1, pw);
}

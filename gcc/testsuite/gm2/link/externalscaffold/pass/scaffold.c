extern  void exit (int);

extern  void m2pim_M2_SYSTEM_init (int argc, char *argv[]);
extern  void m2pim_M2_SYSTEM_fini (void);
extern  void m2pim_M2_M2RTS_init (int argc, char *argv[]);
extern  void m2pim_M2_M2RTS_fini (void);
extern  void m2pim_M2_RTExceptions_init (int argc, char *argv[]);
extern  void m2pim_M2_RTExceptions_fini (void);
extern  void _M2_hello_init (int argc, char *argv[]);
extern  void _M2_hello_fini (void);

extern  void M2RTS_Terminate (void);

static void init (int argc, char *argv[])
{
  m2pim_M2_SYSTEM_init (argc, argv);
  m2pim_M2_M2RTS_init (argc, argv);
  m2pim_M2_RTExceptions_init (argc, argv);
  _M2_hello_init (argc, argv);
}

static void finish (void)
{
  m2pim_M2RTS_Terminate ();
  _M2_hello_fini ();
  m2pim_M2_RTExceptions_fini ();
  m2pim_M2_M2RTS_fini ();
  m2pim_M2_SYSTEM_fini ();
  exit (0);
}

int main (int argc, char *argv[])
{
  init (argc, argv);
  finish ();
  return (0);
}

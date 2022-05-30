extern  void exit(int);

extern  void _M2_SYSTEM_init (int argc, char *argv[]);
extern  void _M2_SYSTEM_finish (void);
extern  void _M2_M2RTS_init (int argc, char *argv[]);
extern  void _M2_M2RTS_finish (void);
extern  void _M2_RTExceptions_init (int argc, char *argv[]);
extern  void _M2_RTExceptions_finish (void);
extern  void _M2_hello_init (int argc, char *argv[]);
extern  void _M2_hello_finish (void);

extern  void M2RTS_Terminate(void);

static void init (int argc, char *argv[])
{
    _M2_SYSTEM_init (argc, argv);
    _M2_M2RTS_init (argc, argv);
    _M2_RTExceptions_init (argc, argv);
    _M2_hello_init (argc, argv);
}

static void finish (void)
{
   M2RTS_Terminate ();
   _M2_hello_finish ();
   _M2_RTExceptions_finish ();
   _M2_M2RTS_finish ();
   _M2_SYSTEM_finish ();
   exit (0);
}

int main (int argc, char *argv[])
{
   init (argc, argv);
   finish ();
   return (0);
}

extern void __gnat_getenv (char *name, int *len, char **value);
extern void __gnat_setenv (char *name, char *value);
extern char **__gnat_environ (void);
extern void __gnat_unsetenv (char *name);
extern void __gnat_clearenv (void);


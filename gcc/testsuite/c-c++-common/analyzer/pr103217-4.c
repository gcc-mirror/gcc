extern char *strdup (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__, __malloc__, __nonnull__ (1)));

extern void abort (void)
  __attribute__ ((__nothrow__ , __leaf__, __noreturn__));

extern int getopt (int ___argc, char *const *___argv, const char *__shortopts)
  __attribute__ ((__nothrow__ , __leaf__, __nonnull__ (2, 3)));
extern char *optarg;

extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

struct state {
  const char *confpath;
  const char *host;
  const char *port;
  const char *state_dir_prefix;
};

static inline char *xstrdup(const char *s) { 
        char *val = strdup(s);
        if (!val)
                abort();
        return val;
}

int config_init(struct state *config);

int main(int argc, char *argv[]) { 
        int rc;
        struct state state = { 0 };

        config_init(&state);

        if ((rc = getopt(argc, argv, "H:p:")) != -1) {
                switch (rc) {
                case 'H':
                        free((void*)state.host);
                        state.host = xstrdup(optarg);
                        break;
                case 'p':
                        free((void*)state.port);
                        state.port = xstrdup(optarg);
                        break;
                } 
        } 

        free((void*)state.host);
        free((void*)state.port);
        return rc;
}

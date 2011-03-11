// PR c++/46803

int strftime(char *, int, const char *, const struct tm *)
        __attribute__ ((__bounded__(__string__,1,2))); // { dg-warning "ignored" }

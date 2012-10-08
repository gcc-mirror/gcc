// PR c++/46803
// { dg-do compile { target c++11 } }

int strftime(char *, int, const char *, const struct tm *)
        [[gnu::__bounded__(__string__,1,2)]]; // { dg-warning "ignored" }

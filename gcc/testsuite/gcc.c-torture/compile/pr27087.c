extern int ptbl[4];
extern int ctbl[4];
void g(void);

void doViews(void) {
    int  *c = ctbl, *p = ptbl;
    while (1)
  {
        p++;
        c++;
        if (*p)
        {
            if (c == p)
            {
                if (*c)
                    return;
            }
            else
              return;
        }
    }

    g();
}

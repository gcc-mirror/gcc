struct vj {
  char buf[1];
};

void
setjmp (struct vj pl)
{
  setjmp (pl); /* { dg-warning "infinite recursion" } */
}

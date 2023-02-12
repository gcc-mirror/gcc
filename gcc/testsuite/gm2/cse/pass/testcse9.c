typedef void (*procedure) (char *);

void
ReadConv_ReadReal (procedure p, char *ch)
{
  (*p)(ch);
}

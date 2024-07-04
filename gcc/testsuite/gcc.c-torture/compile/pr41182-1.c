/* { dg-additional-options "-fpermissive" } */

typedef long unsigned int size_t;
int _lae_process_opts(char *pr, char *pe)
{ 
  return (strlen ("on") < ((size_t) ((pe-&pr[2])>(strlen("on"))                
                                     ? (pe-&pr[2]) : (strlen("on")))));
}

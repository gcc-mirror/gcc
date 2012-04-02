/* { dg-do compile } */

void Env_FetchObj0AttrOffset (unsigned int NumFields,  int *Status)
{
  int Found = 0;
  if (NumFields)      
    while ((*Status == 0) && NumFields-- > 0 && Found == 0)       
      Found = 1;
}

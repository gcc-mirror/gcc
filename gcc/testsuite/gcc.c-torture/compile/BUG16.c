setgetlen (a)
     int *a;
{
  while (*a++ & 0x80000000)
    ;
}

int
main() {
  struct s
    {
      int a;
      short b;
    } __attribute__((packed)) t;

  if (sizeof (t) != (sizeof(int)+sizeof(short))) 
    {
      return 1;
    }
  else 
    {
      return 0;
    }
}

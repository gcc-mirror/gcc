int main ()
{
  bool b = false;
  int i = b++;
  if (i != false || b != true)
    return 1;
  i = b++;
  if (i != true || b != true)
    return 1;
}

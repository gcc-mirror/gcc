int
f (void)
{
  return (*(volatile unsigned int *)8000) / 3;
}

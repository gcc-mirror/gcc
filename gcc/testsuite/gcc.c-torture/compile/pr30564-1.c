static int spready[] = {0, 1, 2, 3};
void explosion_map (int y)
{
  int i;
  for (i = 0; i < 4; i++)
    if (y * spready[i] < 0)
      break;
}
void explosion (void)
{
  int i;
  explosion_map (0);
  for (i = 0; i < 2; i++)
    continue;
}

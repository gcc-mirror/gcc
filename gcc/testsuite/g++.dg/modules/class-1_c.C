import One;

int y = sizeof (Bob::Y);

int Foo (Bob::Y *ptr)
{
  return ptr->a + ptr->b;
}

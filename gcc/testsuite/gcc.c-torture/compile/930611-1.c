float
f (float a1)
{
  union { float f; int l; } fl1;
  fl1.f = a1;
  return fl1.l ? 1.0 : a1;
}

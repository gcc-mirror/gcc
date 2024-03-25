struct qi {
  union {
    int hj;
    float sl;
  };
};

void
i2 (struct qi *la)
{
  if (la->hj == 0)
    la->sl = 0.0f;
}

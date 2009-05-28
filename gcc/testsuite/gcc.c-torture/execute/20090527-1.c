typedef enum { POSITION_ASIS, POSITION_UNSPECIFIED } unit_position;

typedef enum { STATUS_UNKNOWN, STATUS_UNSPECIFIED } unit_status;

typedef struct
{
  unit_position position;
  unit_status status;
} unit_flags;

extern void abort (void);

void
new_unit (unit_flags * flags)
{
  if (flags->status == STATUS_UNSPECIFIED)
    flags->status = STATUS_UNKNOWN;

  if (flags->position == POSITION_UNSPECIFIED)
    flags->position = POSITION_ASIS;

  switch (flags->status)
    {
    case STATUS_UNKNOWN:
      break;

    default:
      abort ();
    }
}

int main()
{
  unit_flags f;
  f.status = STATUS_UNSPECIFIED;
  new_unit (&f);
  return 0;
}

void _bfd_abort (const char *);
void
_bfd_xcoff_canonicalize_dynamic_reloc (unsigned long long l_symndx)
{
  if (l_symndx < 3)
    {
      switch (l_symndx)
      {
        case 0:
        case 1:
         break;
        case 2:
         _bfd_abort ("HI");
    }
  }
}


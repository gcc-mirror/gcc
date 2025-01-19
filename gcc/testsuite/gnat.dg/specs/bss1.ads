package Bss1 is

  I : Integer := 0 with Linker_Section => ".bss"; -- { dg-error "no initializers" }

end Bss1;

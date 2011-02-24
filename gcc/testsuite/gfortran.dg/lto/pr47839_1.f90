MODULE PEC_mod
CONTAINS
SUBROUTINE PECapply(Ex)
USE globalvar_mod, ONLY : xstop
real(kind=8), dimension(1:xstop), intent(inout) :: Ex
END SUBROUTINE PECapply
END MODULE PEC_mod

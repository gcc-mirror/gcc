! { dg-options "-O2 -fgraphite-identity" }

subroutine gentrs (ptrst, ncls, xmin, dcls, xdont, ndon) 
do icls1 = 1, ncls
   prec:    do
      select case (isns)
      case (-1)
         do icls = icls1, 1, -1
         enddo
      case (+1)
         do icls = icls1, ncls
            if (xale > rtrst (icls1, icls)) then
            endif
         enddo
      end select
   enddo prec
enddo
contains
real function genuni (jsee)
end function genuni
end subroutine gentrs

! Check that OpenMP conditional compilations sentinels ('!$ ') are ignored

c$ bogus
!$ bogus
*$ bogus
c$    bogus
!$    bogus
*$    bogus

c$a23 bogus
!$ a  bogus
*$12a bogus

c$ 1  bogus
!$ 22 bogus
*$34  bogus

c$bogus
!$bogus
*$bogus

c$ acc bogus
!$ acc bogus
*$ acc bogus

c$ acc bogus
!$ acc bogus
*$ acc bogus

      end

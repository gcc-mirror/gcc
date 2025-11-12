! { dg-do run }
! { dg-options "-std=f2008" }
! PR36725 Compile time error for g0 edit descriptor
character(30) :: line
write(line, '(g0.3)') 0.1
if (line.ne."0.100") STOP 1
write(line, '(g0.9)') 1.0
if (line.ne."1.00000000") STOP 2
write(line, '(g0.5)') 29.23
if (line.ne."29.230") STOP 3
write(line, '(g0.8)') -28.4
if (line.ne."-28.400000") STOP 4
write(line, '(g0.8)') -0.0001
if (line.ne."-0.99999997E-04") STOP 5
end


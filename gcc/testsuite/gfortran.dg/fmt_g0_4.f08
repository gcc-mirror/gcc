! { dg-do compile }
! { dg-options "-std=f2008" }
! PR36725 Compile time error for g0 edit descriptor
character(30) :: line
write(line, '(g0.3)') 0.1
if (line.ne."      1.000E-01") STOP 1
write(line, '(g0.9)') 1.0
if (line.ne."1.000000000E+00") STOP 2
write(line, '(g0.5)') 29.23
if (line.ne."    2.92300E+01") STOP 3
write(line, '(g0.8)') -28.4
if (line.ne."-2.83999996E+01") STOP 4
write(line, '(g0.8)') -0.0001
if (line.ne."-9.99999975E-05") STOP 5
end

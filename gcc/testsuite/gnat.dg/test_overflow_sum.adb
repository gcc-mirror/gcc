--  { dg-do run }
--  { dg-options "-gnato" }

procedure test_overflow_sum is
   pragma Unsuppress (Overflow_Check);
   function sum (a, b, c, d, e, f, g, h, i, j, k, l, m,
                 n, o, p, q, r, s, t, u, v, w, x, y, z : Integer)
                 return Integer
   is
   begin
      return a + b + c + d + e + f + g + h + i + j + k + l + m
           + n + o + p + q + r + s + t + u + v + w + x + y + z;
   end;

   f : integer;
begin
   f := sum (a => -2**31, b =>      1, c =>  2**31 - 1,  -- 0
             d =>      1, e => -2**31, f =>  2**31 - 1,  -- 0
             g =>   2**0, h =>      2, i =>          4,  -- 2**3 - 1
             j =>   2**3, k =>   2**4, l =>       2**5,  -- 2**6 - 1
             m =>   2**6, n =>   2**7, o =>       2**8,  -- 2**9 - 1
             p =>   2**9, q =>  2**10, r =>      2**11,  -- 2**12 - 1
             s =>  2**12, t =>  2**13, u =>      2**14,  -- 2**15 - 1
             v =>  2**15, w =>  2**16, x =>      2**17,  -- 2**18 - 1
             y =>  2**31 - 2**18,      z =>          0); -- 2**31 - 1

   if f /= 2**31 - 1 then
      raise Program_Error;
   end if;

   begin
      f := sum (a =>      f, b => -2**31, c =>          1,  -- 0
                d => -2**31, e =>      1, f =>          f,  -- 0
                g =>   2**0, h =>      2, i =>          4,  -- 2**3 - 1
                j =>   2**3, k =>   2**4, l =>       2**5,  -- 2**6 - 1
                m =>   2**6, n =>   2**7, o =>       2**8,  -- 2**9 - 1
                p =>   2**9, q =>  2**10, r =>      2**11,  -- 2**12 - 1
                s =>  2**12, t =>  2**13, u =>      2**14,  -- 2**15 - 1
                v =>  2**15, w =>  2**16, x =>      2**17,  -- 2**18 - 1
                y =>  2**31 - 2**18,      z =>          1); -- 2**31 (overflow)
      raise Program_Error;
   exception
      when Constraint_Error => null;
   end;
end test_overflow_sum;

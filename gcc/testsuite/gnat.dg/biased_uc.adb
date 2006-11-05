-- { dg-do run }
-- { dg-options "-gnatws" }

with Unchecked_Conversion;
procedure biased_uc is
begin
    --  Case (f) target type is biased, source is unbiased

    declare 
       type a is new integer range 0 .. 255; 
       for a'size use 8;

       type b is new integer range 200 .. 455; 
       for b'size use 8;

       av : a; 
       bv : b; 

       for av'size use 8;
       for bv'size use 8;

       function a2b is new Unchecked_Conversion (a,b);

    begin   
       bv := a2b (200);
       if bv = 200 then
          raise Program_Error;
       end if; 
    end;    

    --  Case (g) target type is biased, source object is biased

    declare 
       type a is new integer range 1 .. 256; 
       for a'size use 16; 

       type b is new integer range 1 .. 65536;
       for b'size use 16;

       av : a;
       bv : b;

       for av'size use 8;
       for bv'size use 16;

       function a2b is new Unchecked_Conversion (a,b);

    begin
       bv := a2b (1);
       if bv /= 2 then
          raise Program_Error;
       end if;
    end;
end;

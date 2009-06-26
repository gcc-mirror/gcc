-- { dg-do run }

with Interfaces; use Interfaces;

procedure Array6 is

   type buf_t is array (unsigned_32 range <>) of character;
   type v_str_t (first, last : unsigned_32) is
      record
         buf : buf_t (first .. last) := (others => ' ');
      end record;
   type v_str_ptr_t is access all v_str_t;

   v_str : v_str_ptr_t;

   function build_v_str (f, l : unsigned_32) return v_str_ptr_t is
      vp : v_str_ptr_t := new v_str_t (f, l);
   begin
      return vp;
   end;

begin
   v_str := build_v_str (unsigned_32'last/2 - 256,
                         unsigned_32'last/2 + 1024*1024);
end;

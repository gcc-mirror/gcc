--  { dg-do run }

with Interfaces; use Interfaces;

procedure Access7 is
   type t_p_string is access constant String;
   subtype t_hash is Unsigned_32;

   -- Return a hash value for a given string
   function hash(s: String) return t_hash is
      h: t_hash := 0;
      g: t_hash;
   begin
      for i in s'Range loop
         h := Shift_Left(h, 4) + t_hash'(Character'Pos(s(i)));
         g := h and 16#F000_0000#;
         if (h and g) /= 0 then
            h := h xor ((Shift_Right(g, 24) and 16#FF#) or g);
         end if;
      end loop;
      return h;
   end hash;

   type hash_entry is record
      v: t_p_string;
      hash: t_hash;
      next: access hash_entry;
   end record;

   type hashtable is array(t_hash range <>) of access hash_entry;

   protected pool is
      procedure allocate (sp: out t_p_string; s: String; h: t_hash);
   private
      tab: hashtable(0..199999-1) := (others => null);
   end pool;

   protected body pool is
      procedure allocate(sp: out t_p_string; s: String; h: t_hash) is
         p: access hash_entry;
         slot: t_hash;
      begin
         slot := h mod tab'Length;
         p := tab(slot);
         while p /= null loop
            -- quickly check hash, then length, only then slow comparison
            if p.hash = h and then p.v.all'Length = s'Length
              and then p.v.all = s
            then
               sp := p.v;   -- shared string
               return;
            end if;
            p := p.next;
         end loop;
         -- add to table
         p := new hash_entry'(v    => new String'(s),
                              hash => h,
                              next => tab(slot));
         tab(slot) := p;  --  { dg-warning "accessibility check fails|Program_Error will be raised at run time" }
         sp := p.v;     -- shared string
      end allocate;
   end pool;

   -- Return the pooled string equal to a given String
   function new_p_string(s: String) return t_p_string is
      sp: t_p_string;
   begin
      pool.allocate(sp, s, hash(s));
      return sp;
   end new_p_string;

   foo_string : t_p_string;
begin
   foo_string := new_p_string("foo");
   raise Constraint_Error;
exception
   when Program_Error =>
      null;
end Access7;

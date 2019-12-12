--  { dg-do compile }
procedure Predicate9 is
  function Num (x : Integer) return Integer is (X + 1);
  function name (X : String) return Integer is (X'Size);
  function Post (One : Integer; Two : Integer) return Boolean;

  generic
     type T is private;
  procedure Pro (Z : Integer) with Post =>
    Post (Num (5), Two => Name ("yeah"));

  function Post (One : Integer; Two : Integer) return Boolean
  is (True);

  procedure Pro (Z : Integer) is
  begin
     null;
  end Pro;
begin
   null;
end Predicate9;

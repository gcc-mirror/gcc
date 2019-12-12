package overload2_q is
  type t is null record;
  function "=" (this, that: t) return boolean;
end;

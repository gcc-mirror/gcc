package Aggr23_TT is
   type T (D : not null access Integer) is null record;
   type TA is access T;
end;

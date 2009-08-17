package itype is
   generic
      type T is private;
   function G return not null access constant T;
end itype;

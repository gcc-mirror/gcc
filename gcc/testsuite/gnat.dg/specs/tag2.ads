-- { dg-do compile }

package tag2 is
   type I is synchronized interface;
   type T1 is tagged;
   type T2 is tagged;
   type T3 is tagged;
   type T4 is tagged;
   type T5 is tagged;
   type T6 is tagged;
   protected type T1 is end T1; -- { dg-error "must be a tagged type" }
   task type T2;                -- { dg-error "must be a tagged type" }
   type T3 is null record;      -- { dg-error "must be a tagged type" }
   task type T4 is new I with end;
   protected type T5 is new I with end;
   type T6 is tagged null record;
end tag2;

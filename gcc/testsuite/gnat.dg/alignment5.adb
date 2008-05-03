-- { dg-do compile }
-- { dg-options "-gnatws -fdump-tree-gimple" }

procedure Alignment5 is

   type MY_REC is
     record
       A1 : INTEGER range -3 .. 3 ; -- symmetric
       A2 : BOOLEAN ;
       A3 : INTEGER range 0 .. 15 ; -- positive
       A4 : INTEGER range 10 .. 100 ; -- arbitrary
       A5 : BOOLEAN ;  --5
     end record ;

   for MY_REC use
     record
       A1 at 0 range 0 .. 2 ;
       A2 at 0 range 3 .. 3 ;
       A3 at 0 range 4 .. 7 ;
       A4 at 0 range 8 .. 15 ;
       A5 at 0 range 16 .. 16 ;
     end record ;

   A_REC, B_REC : MY_REC;

begin
   A_REC := B_REC;
end;

-- { dg-final { scan-tree-dump-not "\.F" "gimple" } }
-- { dg-final { cleanup-tree-dump "gimple" } }

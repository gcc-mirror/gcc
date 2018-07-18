-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

with Interfaces;

procedure Atomic9 is

   Register : Interfaces.Unsigned_32;

   type Interrupt_ID is range 0 .. 44;

   procedure Trigger_Interrupt (IRQ : Interrupt_ID) is
      NVIC_STIR : Interrupt_ID
      with
        Import,
        Atomic,
        Size    => 32,
        Address => Register'Address;

   begin
      NVIC_STIR := IRQ;
   end Trigger_Interrupt;

begin
   Register := 16#ffff_ffff#;
   Trigger_Interrupt (1);
end;

-- { dg-final { scan-tree-dump "atomic_store_4" "gimple" } }

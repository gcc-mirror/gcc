// { dg-do run }
// { dg-require-effective-target c++11 }
// { dg-options "-O2" }

typedef __UINT64_TYPE__ u64;

struct SmallDenseMap {
  static constexpr u64 EmptyKey = 0xC0FFEUL;
  struct V { u64 v; };

  bool contains(u64 Val) {
    V *TheSlot = nullptr;
    return (LookupSlotFor(Val, TheSlot) ? 1 : 0);
  }

  void try_emplace(u64 Key) {
    V *TheSlot = nullptr;
    if (LookupSlotFor(Key, TheSlot))
      return;

    // Otherwise, insert the new element.
    InsertIntoSlot(TheSlot, Key);
  }

  void moveFromOldSlots(V *OldSlotsBegin, V *OldSlotsEnd) {
    Size = 0;

    V *B_ = u.o.Slots;
    V *E_ = B_ + u.o.Capacity;
    for (; B_ != E_; ++B_)
      B_->v = EmptyKey;

    // Insert all the old elements.
    V *O = OldSlotsBegin;
    V *E = OldSlotsEnd;
    for (; O != E; ++O) {
      if (O->v != EmptyKey) {
        // Insert the key/value into the new table.
        V * N = nullptr;
        LookupSlotFor(O->v, N);
        N->v = O->v;
        Size++;
      }
    }
  }

  void InsertIntoSlot(V *TheSlot, u64 Key) {
    unsigned NewSize = Size + 1;
    unsigned Capacity = getCapacity();
    // Make sure we always keep at least one Empty value
    if (NewSize >= Capacity) {
      //fprintf(stderr, "GROW: size=%u capacity=%u -> ...\n", Size, Capacity);
      grow();
      LookupSlotFor(Key, TheSlot);
      Capacity = getCapacity();
      //fprintf(stderr, "GROW: ... -> size=%u capacity=%u\n", NewSize, Capacity);
    }

    Size++;

    TheSlot->v = Key;
  }

  bool LookupSlotFor(u64 Val,
                       V *&FoundSlot) {
    V *SlotsPtr = getSlots();
    const unsigned Capacity = getCapacity();

    for (unsigned i = 0; i < Capacity; ++i) {
      V *ThisSlot = SlotsPtr + i;
      if (Val == ThisSlot->v) {
        FoundSlot = ThisSlot;
        return true;
      }

      if (ThisSlot->v == EmptyKey) {
        FoundSlot = ThisSlot;
        return false;
      }
    }
    // Guarantee that within an array there is a match
    // or Empty value where to insert a new vaue.
    __builtin_trap();
  }

  // Needs to bea at least 1 to hld one empty value
  static constexpr unsigned InlineSlots = 2;

  bool Small;
  unsigned Size;

  struct LargeRep {
    V *Slots;
    unsigned Capacity;
  };

  union {
      V i[InlineSlots]; // Small = true
      LargeRep o;       // Small = false
  } u;

  explicit SmallDenseMap() : Small(true), Size(0) {
    Size = 0;

    V *B = u.i;
    V *E = B + InlineSlots;
    for (; B != E; ++B)
      B->v = EmptyKey;
  }

  void grow() {
    // assert:
    if (!Small) __builtin_trap();

    // First move the inline Slots into a temporary storage.
    V TmpStorage[InlineSlots];
    V *TmpBegin = TmpStorage;
    V *TmpEnd = TmpBegin;

    // Loop over the Slots, moving non-empty, non-tombstones into the
    // temporary storage. Have the loop move the TmpEnd forward as it goes.
    V *P = u.i;
    V *E = P + InlineSlots;
    for (; P != E; ++P) {
        if (P->v != EmptyKey) {
            TmpEnd->v = P->v;
            ++TmpEnd;
        }
    }

    Small = false;
    u.o = LargeRep{new V[128], 128};
    moveFromOldSlots(TmpBegin, TmpEnd);
  }

  V *getSlots() {
    if (Small) {
      V * inl = u.i;
      return inl;
    }
    else {
      LargeRep * rep = &u.o;
      return rep->Slots;
    }
  }

  unsigned getCapacity() {
    if (Small) {
      return InlineSlots;
    }
    else {
      LargeRep * rep = &u.o;
      return rep->Capacity;
    }
  }
};

#pragma GCC optimize(0)

struct P {
    u64 f;
    bool s;
};

static u64 ws = 0;
static P WorkList[128];

__attribute__((noipa))
static void popupateIni() {
  for (u64 Var : (u64[]){8,7,6,5,4,3,0}) {
    WorkList[ws++] = P{Var, false};
  }
}

__attribute__((noipa))
static void checkCycle(u64 Var) {
    // Detect cycles.
    static bool seen[256];
    if (Var >= 256 || seen[Var]) __builtin_trap();
    seen[Var] = true;
}


__attribute__((noipa))
static void populateDeps(u64 Var) {

    WorkList[ws++] = P{Var, true};
    if (Var == 8)
        WorkList[ws++] = P{0, false};
}


__attribute__((noipa)) __attribute__((optimize(3)))
static void bug() {

  // triggers growth on insert
  SmallDenseMap Visited;

  popupateIni();

  while (ws > 0) {
    P Item = WorkList[--ws];
    u64 Var = Item.f;
    bool visitedAllDependencies = Item.s;

    if (Visited.contains(Var)) {
      continue;
    }

    if (visitedAllDependencies) {
      Visited.try_emplace(Var);
      continue;
    }

    checkCycle(Var);
    populateDeps(Var);
  }
}

__attribute__((noipa))
int main() {
    bug();
}

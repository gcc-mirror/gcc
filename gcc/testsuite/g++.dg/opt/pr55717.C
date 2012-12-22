// PR debug/55717
// { dg-do compile }
// { dg-options "-O -g" }

struct DebugOnly {};
template <class T>
struct StripConst { typedef T result; };
class TempAllocPolicy {};
template <class T>
class HashTableEntry
{
  unsigned keyHash;
  template <class, class, class>
  friend class HashTable;
  T t;
  void setLive (unsigned hn) { keyHash = hn; }
};
template <class T, class HashPolicy, class>
struct HashTable
{
  typedef typename HashPolicy::KeyType Key;
  typedef typename HashPolicy::Lookup Lookup;
  typedef HashTableEntry <T> Entry;
  struct Range
  {
    Range () {}
    Entry *cur, end;
    bool empty () { return false; }
    T front () { return T (); }
  };
  struct Enum : public Range
  {
    HashTable table;
    bool removed;
    template <class Map>
    Enum (Map map) : Range (map.all ()), table (map.impl), removed () {}
    void rekeyFront (Lookup l, Key)
    {
      T t = this->cur->t;
      table.putNewInfallible (l, t);
    }
    void rekeyFront (Key k)
    {
      rekeyFront (k, k);
    }
  };
  unsigned entryCount;
  unsigned sCollisionBit;
  unsigned prepareHash (Lookup l)
  {
    unsigned keyHash (HashPolicy::hash (l));
    return keyHash & sCollisionBit;
  }
  static Entry *entryp;
  Entry *findFreeEntry (unsigned) { return entryp; }
  void putNewInfallible (Lookup l, T)
  {
    unsigned keyHash = prepareHash (l);
    Entry *entry = findFreeEntry (keyHash);
    entry->setLive (keyHash);
    entryCount++;
  }
};
template <class Key>
struct HashMapEntry { Key key; };
template <class Key, class Value, class HashPolicy = DebugOnly, class AllocPolicy = TempAllocPolicy>
struct HashMap
{
  typedef HashMapEntry <Key> Entry;
  struct MapHashPolicy : HashPolicy
  {
    typedef Key KeyType;
  };
  typedef HashTable <Entry, MapHashPolicy, AllocPolicy> Impl;
  Impl impl;
  typedef typename Impl::Range Range;
  Range all () { return Range (); }
  typedef typename Impl::Enum Enum;
};
class FreeOp;
struct AllocationSiteKey;
typedef HashMap <AllocationSiteKey, DebugOnly, AllocationSiteKey, TempAllocPolicy> AllocationSiteTable;
struct TypeCompartment
{
  AllocationSiteTable *allocationSiteTable;
  void sweep (FreeOp *);
};
struct JSScript { unsigned *code; };
bool IsScriptMarked (JSScript **);
struct AllocationSiteKey
{
  JSScript *script;
  unsigned offset : 24;
  int kind;
  typedef AllocationSiteKey Lookup;
  static unsigned hash (AllocationSiteKey key) { return (long (key.script->code + key.offset)) ^ key.kind; }
};
void
TypeCompartment::sweep (FreeOp *)
{
  for (AllocationSiteTable::Enum e (*allocationSiteTable); !e.empty ();)
    {
      AllocationSiteKey key = e.front ().key;
      IsScriptMarked (&key.script);
      e.rekeyFront (key);
    }
}

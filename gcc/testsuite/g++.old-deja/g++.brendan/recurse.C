// { dg-do assemble  }
// GROUPS passed recursive-aborts
// types
typedef unsigned int DBflag;   // for storing user flag value
typedef unsigned long DBoffset; // 32-bit unsigned integer
typedef DBoffset DBsize;  // type for storing sizes of objects
typedef unsigned char DBbyte;   // 8-bit unsigned char

class DBlink
{
protected:
  DBbyte link[4];       // hold link in portable MSB first format
public:
  DBlink(DBoffset = 0, DBflag = 0);
  DBlink &operator=(const DBlink &);
  DBlink &operator=(DBoffset);
  operator DBoffset();
  operator const DBbyte *() { return link; }
  void set_flag() { link[0] |= 0x80; }
  void reset_flag() { link[0] &= 0x7f; }
  int test_flag() const { return (link[0] & 0x80) != 0; }
};

typedef DBlink DBsizerec;       // hold data record size in portable format

// constants
const DBoffset DB_NULL = 0;

class DBlinkrec
{
protected:
  // offsets are stored with MSB in link[0]
  DBlink l;  // offset into link file of right child - MSB = red bit
  DBlink r;  // offset into link file of left child - MSB = delete
  DBlink d;  // offset into parallel data file - MSB = user flag
public:
  DBlinkrec():l(DB_NULL), r(DB_NULL), d(DB_NULL) {}
  void make_red() // set link to red
  { l.set_flag(); }
  void make_black() // set link to black
  { l.reset_flag(); }
  int is_red() const // indicates whether this is a red link
  { return l.test_flag(); }
  void set_discard() // set discard flag
  { r.set_flag(); }
  void reset_discard() // reset discard flag
  { r.reset_flag(); }
  int is_discarded() const // check discard flag
  { return r.test_flag(); }
  void set_flag() // set user flag
  { d.set_flag(); }
  void reset_flag() // reset user flag
  { d.reset_flag(); }
  int is_flag() const // check user flag
  { return d.test_flag(); }

  friend class DataBase;
};

class DBpathrec : public DBlinkrec
{
  DBoffset offset;    // offset of link record in LNK file
public:
  DBpathrec():offset(DB_NULL) { }
  DBpathrec(DBoffset off, const DBlinkrec &lr):offset(off), DBlinkrec(lr) {}
  operator DBoffset() { return offset; }
  DBpathrec &operator=(DBoffset off) { offset = off; return *this; }
  DBpathrec &operator=(const DBpathrec &pr)
  { offset = pr.offset; (DBlinkrec)*this = (DBlinkrec)pr; return *this; }

  friend class DataBase;
};

int main()
{
  DBpathrec a(), b();

  a = b;// { dg-error "" }  non-lvalue in assignment.*
}


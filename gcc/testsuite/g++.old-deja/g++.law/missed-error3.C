// Build don't link: 
// GROUPS passed missed-error
// missed-error file
// From: Neil Wilson <csf004@cch.coventry.ac.uk>
// Date:     Tue, 28 Apr 92 13:53:54 WET DST
// Subject:  g++ version 2.1 bugs
// Message-ID: <15717.9204281253@cch.coventry.ac.uk>


// enum bool { FALSE = 0, TRUE = 1 };

typedef int T;

class Traversable {
public:
    virtual const T item() const = 0;
    virtual const bool off() const = 0;
    virtual ~Traversable() { };
};

class Chain : public Traversable {
public:
    virtual const int count() const = 0;
    virtual const bool empty() const = 0;
    virtual void forth() const = 0;
    virtual const bool isfirst() const = 0;
    virtual const bool islast() const = 0;
    virtual const int position() const = 0;
    virtual const T first() const = 0;
    virtual const T last() const = 0;
    virtual const T i_th(const int index) const = 0;
    virtual void start() const = 0;
    virtual void back() const = 0;
    virtual void finish() const = 0;
    virtual void move(const int index) const = 0;
    virtual void go(const int index) const = 0;
    virtual void put(const T value) = 0;
    virtual void put_i_th(const T value, const int index) = 0;
    virtual void swap(const int index) = 0;
    virtual void wipe_out() = 0;
};
class List : public Chain {
protected:
    int item_count;
    int cursor_position;
    virtual void go_offleft() const = 0;
    virtual void go_offright() const = 0;
    virtual void copy(const List& other) = 0;
public:
    List() : item_count(0), cursor_position(0) { };
    virtual const int count() const;
    virtual const bool empty() const;
    virtual const bool isfirst() const;
    virtual const bool islast() const;
    virtual const bool offleft() const;
    virtual const bool offright() const;
    virtual const bool off() const;
    virtual const int position() const;
    virtual const T first() const;
    virtual const T last() const;
    virtual const T i_th(const int index) const;
    virtual void start() const;
    virtual void forth() const;
    virtual void back() const;
    virtual void finish() const;
    virtual void move(const int index) const;
    virtual void go(const int index) const;
    friend const bool operator==(const List& left,
	const List& right);
    friend const bool operator!=(const List& left,
	const List& right);
    virtual void put_i_th(const T value, const int index);
    virtual void swap(const int index);
};
typedef int T;
class Array {
private:
    int lower_index;
    int upper_index;
    T *array;
protected:
    virtual void allocate(const int minindex, const int maxindex);
    virtual void copy(const Array&other);
public:
    Array(const int minindex, const int maxindex);
    Array(const Array& other);
    virtual const int count() const;
    virtual const int lower() const;
    virtual const int upper() const;
    virtual const T item(const int index) const;
      const T Array::operator[](const int index); // ERROR - qualification ignored
    virtual const bool valid_index(const int index) const;
    virtual const bool empty() const;
    friend const bool operator==(const Array& left, const Array& right);
    friend const bool operator!=(const Array& left, const Array& right);
    virtual void put(const T value, const int index);
    virtual void wipe_out();
    Array& operator=(const Array& other);
    virtual ~Array();
};
class Fixed_List: public List, private Array {
protected:
    virtual void go_offleft() const;
    virtual void go_offright() const;
    virtual void copy(const List& other);
public:
    Fixed_List(const List& other);
    Fixed_List(const int size): Array(1, size) { };
    virtual const bool empty() const;
    virtual const int count() const;
    virtual const T item() const;
    virtual const T i_th(const int index) const;
    virtual void move(const int index) const;
    virtual void put(const T value);
    virtual void put_i_th(const T value, const int index);
    virtual void wipe_out() { };
    Fixed_List& operator=(const List& other);
};

void Fixed_List::go_offleft() const
{
    cursor_position = 0;// ERROR - 
};


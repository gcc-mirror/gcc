// { dg-do run { target c++11 } }
// PR rtl-optimization/95493 comment 8

#include <array>
#include <iostream>

struct Point
{
    std::array<int, 3> array;

    Point(int x, int y, int z) : array{x, y, z} {}
    
    Point(const Point & other) : array{other.array} {} // OK if commented
    //Point(const Point &) = default; // OK

    //Point(Point && other) = default; // OK

    int  operator[] (std::size_t i) const { return array[i]; }
    int& operator[] (std::size_t i)       { return array[i]; }
};

//using Point = std::array<int, 3>; // OK

struct Cell
{
    Point point;
    Cell(Point const& pt) : point(pt) {}
    int   operator[] (std::size_t i) const { return point[i]; }
    int&  operator[] (std::size_t i)       { return point[i]; }
};

//using Cell = Point; // OK

std::ostream & operator<< (std::ostream & out, Cell const& object)
//std::ostream & operator<< (std::ostream & out, Cell object) // Fails with f2 too
{
    for ( std::size_t i = 0; i < 3; ++i )
        out << object[ i ] << " ";
    return out;
}


struct DirIterator
{
    std::size_t dir;
    Cell cell;

    DirIterator(Cell c)
        : dir(0), cell(c)
    {
        find(); // OK if commented
    }

    void find()
    {
        //while (dir < 3) // Fails with f2 too
        while (dir < 3 && (cell[dir] % 2) == 0)
            ++dir;
    }
};

Cell uIncident(Cell c, std::size_t k)
//Cell uIncident(Cell& c, std::size_t k) // OK
{
    --c[k];
    return c;
}

Cell uSpel(Point p)
{
    for (std::size_t i = 0; i < 3; ++i)
        p[i] += p[i] + 1;
    return Cell(p);
}


int main ()
{
    Cell c = uSpel(Point{0, 0, 0}); // Fails
    //Cell c( Point(1, 1, 1) ); // OK

    auto q = DirIterator( c );

    Cell f1 = uIncident( c, q.dir ); // Fails
    //Cell f1 = uIncident( c, 0 ); // OK

    Cell f2 = f1; // f2 is the right cell that f1 should be

    std::cout << "q.dir = " << q.dir << " ; f1 = " << f1 << " ; f2 = " << f2 << std::endl;
    //std::cout << "q.dir = " << q.dir << " ; f1 = " << f1[0] << " " << f1[1] << " " << f1[2] << " ; f2 = " << f2[0] << " " << f2[1] << " " << f2[2] << std::endl; // OK

    for (int i = 0; i < 3; ++i)
      if (f1[i] != f2[i])
        __builtin_abort();
}

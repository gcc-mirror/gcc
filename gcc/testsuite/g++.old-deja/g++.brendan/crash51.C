// { dg-do assemble  }
// GROUPS passed old-abort
class obj;

typedef obj *obj_t;

class obj {
public:
    obj( const obj& o );
};   

extern obj nowhere;

class set: public obj {
    unsigned  bit_vector;
public:
    set( const obj& o );
    set&      operator|=( const int q );
};   

enum pin_enum { E_F, O_C, O_D, O_S, P_D, P_U, R, T, A, C };

set t_q = ( ( ( ( ( ( set( nowhere ) |= E_F ) |= O_C ) |= O_D ) |= O_S )
              |= P_U ) |= P_D ) |= T;

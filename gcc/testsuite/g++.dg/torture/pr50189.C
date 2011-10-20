// { dg-do run }
// { dg-options "-fstrict-enums" }

extern "C" void abort (void);
class CCUTILS_KeyedScalarLevelPosition
{
public:

    typedef enum
    {
        UNINITED = 0,
        AT_BEGIN = 1,
        AT_END = 2,
        AT_KEY = 3

    } position_t;

    bool is_init() const
    { return(m_timestamp != UNINITED); }

    bool is_at_begin() const
    { return(m_timestamp == AT_BEGIN); }

    position_t get_state() const
    {
        return((m_timestamp >= AT_KEY)
             ? AT_KEY
             : ((position_t)m_timestamp));
    }

    void set_at_begin()
    { m_timestamp = AT_BEGIN; }

    unsigned int get_index() const
    { return(m_index); }

    void set_pos(unsigned int a_index, unsigned int a_timestmap)
    {
        m_index = a_index;
        m_timestamp = a_timestmap;
    }

    bool check_pos(unsigned int a_num_entries, unsigned int a_timestamp) const
    {
        if (get_state() != AT_KEY)
            return(false);

        if (m_timestamp != a_timestamp)
            return(false);

        return(m_index < a_num_entries);
    }

    void set_not_init()
    { m_timestamp = 0; }

private:

    unsigned int m_timestamp;
    unsigned int m_index;

};

class CCUTILS_KeyedScalarPosition
{
public:

    CCUTILS_KeyedScalarLevelPosition m_L1;
    CCUTILS_KeyedScalarLevelPosition m_L2;
};

class baz
{
public:
    int *n[20];
    unsigned int m_cur_array_len;
    unsigned int m_timestamp;

    unsigned int _get_timestamp() const
    { return(m_timestamp); }

    bool _check_L1_pos(const CCUTILS_KeyedScalarPosition &a_position) const
    {
        return(a_position.m_L1.check_pos(
                   m_cur_array_len, _get_timestamp()));
    }

    void *next (CCUTILS_KeyedScalarPosition &);
};

void * baz::next (CCUTILS_KeyedScalarPosition &a_position)
{
    if (a_position.m_L1.is_at_begin() || (!a_position.m_L1.is_init()))
    {
        a_position.m_L1.set_pos(0, _get_timestamp());
        a_position.m_L2.set_at_begin();
    }
    else if (!_check_L1_pos(a_position))
        return(0);

    return n[a_position.m_L1.get_index ()];
}

int main (int, char **)
{
    baz obj;
    CCUTILS_KeyedScalarPosition a_pos;
    void *ret;
    int n[5];
    
    obj.n[0] = n;
    obj.m_cur_array_len = 1;
    obj.m_timestamp = 42;
    
    a_pos.m_L1.set_pos (0, 42);
    
    ret = obj.next (a_pos);
    if (ret == 0)
      abort ();
    return 0;
}

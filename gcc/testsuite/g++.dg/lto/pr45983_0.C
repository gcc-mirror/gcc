// PR c++/45983

template <int N>
class T1 {
    int m[N];
    typedef float scalar_type_t;
    typedef scalar_type_t scalar_array_t[1];
    const scalar_array_t &decay(void) const;
};
class T2 {
public:
    float vals[1];
    float get_value(void) const { return vals[0]; }
};
T2 channel_params;
float output_audio(void) {
    return channel_params.get_value();
}

int main(){}

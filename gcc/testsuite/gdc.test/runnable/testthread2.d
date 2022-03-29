// PERMUTE_ARGS:

// Quick, dirty and inefficient AA using linear search, useful for testing.
struct LinearAA(K, V) {
    K[] keys;
    V[] values;

    V opIndex(K key) {
        foreach(i, k; keys) {
            if(k == key) {
                return values[i];
            }
        }

        assert(0, "Key not present.");
    }

    V opIndexAssign(V val, K key) {
        foreach(i, k; keys) {
            if(k == key) {
                return values[i] = val;
            }
        }

        keys ~= key;
        values ~= val;
        return val;
    }

    V* opBinaryRight(string op : "in")(K key) {
        foreach(i, k; keys) {
            if(key == k) {
                return values.ptr + i;
            }
        }

        return null;
    }

    void remove(K key) {
        size_t i = 0;
        for(; i < keys.length; i++) {
            if(keys[i] == key) {
                break;
            }
        }

        assert(i < keys.length);

        for(; i < keys.length - 1; i++) {
            keys[i] = keys[i + 1];
            values[i] = values[i + 1];
        }

        keys = keys[0..$ - 1];
        values = values[0..$ - 1];
    }

    size_t length() {
        return values.length;
    }
}

extern (C) int rand();

uint random(const uint max = uint.max)
{
    version (Windows)
    {
        // RAND_MAX is quite low for windows, extend the range by
        // abusing multiple random values and rounding errors.
        const a = rand(), b = rand();
        const c = a < b ? double(a) / b : double(b) / a;
        return (cast(int) (c * max)) % max;
    }
    else
        return rand() % max;
}

void main()
{
    foreach(iter; 0..10) {  // Bug only happens after a few iterations.

        uint[size_t] builtin;
        LinearAA!(size_t, uint) linAA;
        uint[] nums = new uint[100_000];
        foreach(ref num; nums) {
            num = random();
        }

        foreach(i; 0..10_000) {
            auto index = random(cast(uint) nums.length);
            if(index in builtin) {
                assert(index in linAA);
                assert(builtin[index] == nums[index]);
                assert(linAA[index] == nums[index]);
                builtin.remove(index);
                linAA.remove(index);
            } else {
                assert(!(index in linAA));
                builtin[index] = nums[index];
                linAA[index] = nums[index];
            }
        }

        assert(builtin.length == linAA.length);
        foreach(k, v; builtin) {
            assert(k in linAA);
            assert(*(k in builtin) == *(k in linAA));
            assert(linAA[k] == v);
        }
    }
}

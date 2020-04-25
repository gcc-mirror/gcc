// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:

import std.algorithm : map;
import std.random : Random, uniform, unpredictableSeed;
import std.range : repeat;
import std.stdio : writeln;

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

    V* opIn_r(K key) {
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

void main() {
    Random gen;
    uint[] seed;
    gen.seed(map!((a) {
        seed ~= unpredictableSeed;
        return seed[$-1]; })(repeat(0)));
    writeln(seed);
    foreach(iter; 0..10) {  // Bug only happens after a few iterations.
        writeln(iter);
        uint[size_t] builtin;
        LinearAA!(size_t, uint) linAA;
        uint[] nums = new uint[100_000];
        foreach(ref num; nums) {
            num = uniform(0U, uint.max, gen);
        }

        foreach(i; 0..10_000) {
            auto index = uniform(0, nums.length, gen);
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


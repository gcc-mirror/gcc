fn main() {
    // const, inferred array
    let const_inferred_array = [0, 1, 2, 3];
    // const, inferred, copied array 
    let const_inferred_copied_array = [0; 4];

    // const index of const, inferred array
    let const_inferred_index = const_inferred_array[1];
    // variable index of const, inferred array
    let some_var = 3;
    let const_inferred_index_var = const_inferred_array[some_var];

    // const index of const, inferred, copied array
    let const_inferred_copied_index = const_inferred_copied_array[1];
    // variable index of const, inferred array
    let const_inferred_copied_index_var = const_inferred_copied_array[some_var];

    // mut, inferred array
    let mut mut_inferred_array = [0, 1, 2, 3];
    // mut, inferred, copied array
    let mut mut_inferred_copied_array = [0; 6];

    // const index of mut, inferred array
    let mut_inferred_index = mut_inferred_array[1];
    // variable index of mut, inferred array
    let mut_inferred_index_var = mut_inferred_array[some_var];

    // const index of mut, inferred, copied array
    let mut_inferred_copied_index = mut_inferred_copied_array[1];
    // variable index of mut, inferred array
    let mut_inferred_copied_index_var = mut_inferred_copied_array[some_var];

    // const, typed array
    let const_typed_array: [i32; 5] = [0, 1, 2, 3, 4];
    // const, typed, copied array
    let const_typed_copied_array: [i32; 4] = [2; 4];

    // const index of const, typed array
    let const_typed_index = const_typed_array[1];
    // variable index of const, typed array
    let const_typed_index_var = const_typed_array[some_var];

    // const index of const, typed, copied array
    let const_typed_copied_index = const_typed_copied_array[1];
    // variable index of const, typed array
    let const_typed_copied_index_var = const_typed_copied_array[some_var];

    // mut, typed array
    let mut mut_typed_array: [i32; 4] = [0, 1, 2, 3];
    // mut, typed, copied array
    let mut mut_typed_copied_array: [i32; 4] = [0; 4];

    // const index of mut, typed array
    let mut_typed_index = mut_typed_array[1];
    // variable index of mut, typed array
    let mut_typed_index_var = mut_typed_array[some_var];

    // const index of mut, typed, copied array
    let mut_typed_copied_index = mut_typed_copied_array[1];
    // variable index of mut, typed array
    let mut_typed_copied_index_var = mut_typed_copied_array[some_var];


    // index + 1 expression
    let some_thing = mut_inferred_copied_array[some_var + 1];
}